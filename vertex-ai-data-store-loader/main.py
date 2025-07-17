# Copyright 2025 Google LLC
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     https://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License. 
import os
from datetime import datetime, timezone
import logging
from google.cloud import bigquery, storage
from google.cloud import discoveryengine_v1 as discoveryengine 
from google.api_core import exceptions
import json
from dotenv import load_dotenv

# Load environment variables from .env file
load_dotenv()

# --- Configuration (from environment variables) ---
GCP_PROJECT = os.getenv("GCP_PROJECT")
GCS_TIMESTAMP_BUCKET_NAME = os.getenv("GCS_TIMESTAMP_BUCKET_NAME")
GCS_STATE_FILE = os.getenv("GCS_STATE_FILE")
GCS_SOURCE_DATA_BUCKET_NAME = os.getenv("GCS_SOURCE_DATA_BUCKET_NAME")
BQ_DATASET = os.getenv("BQ_DATASET")
BQ_LINK_TABLES = os.getenv("BQ_LINK_TABLES")
BQ_TIMESTAMP_COLUMN = os.getenv("BQ_TIMESTAMP_COLUMN")
DISCOVERY_ENGINE_DATA_STORE_ID = os.getenv("DISCOVERY_ENGINE_DATA_STORE_ID")

# --- Initialize Clients ---
storage_client = storage.Client()
bq_client = bigquery.Client()
state_bucket = storage_client.bucket(GCS_TIMESTAMP_BUCKET_NAME)
state_blob = state_bucket.blob(GCS_STATE_FILE)
source_data_bucket = storage_client.bucket(GCS_SOURCE_DATA_BUCKET_NAME) # New: Initialize bucket for source data

# --- Set up logging ---
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')

def create_document(client: discoveryengine.DocumentServiceClient, data_store: str, document_id: str, mime_type: str, row_json = str, gcs_uri = str):
    document = discoveryengine.Document()
    document.json_data = row_json
    document.content.uri = gcs_uri
    document.content.mime_type = mime_type

    request = discoveryengine.CreateDocumentRequest(
        parent=f"projects/{GCP_PROJECT}/locations/global/dataStores/{data_store}/branches/default_branch",
        document_id=document_id,
        document=document)
    
    created_doc = client.create_document(request=request)
    
    return created_doc

def get_last_timestamp():
    """
    Retrieves the last processed timestamp from a GCS file.
    Returns a default old timestamp if the file doesn't exist.
    """
    try:
        last_timestamp_str = state_blob.download_as_text()
        print(f"Found last processed timestamp: {last_timestamp_str}")
        return last_timestamp_str
    except exceptions.NotFound:
        # If the file doesn't exist, this is the first run.
        # Use a very old timestamp to get all records.
        print("State file not found. Starting from 1971.")
        return "1971-01-01T00:00:00Z"

def write_last_timestamp(timestamp_to_write):
    """Writes the given timestamp to the GCS state file."""
    if not isinstance(timestamp_to_write, str):
        # Ensure timestamp is in ISO 8601 format with Z for UTC.
        timestamp_to_write = timestamp_to_write.strftime('%Y-%m-%dT%H:%M:%S.%fZ')

    state_blob.upload_from_string(timestamp_to_write)
    print(f"Successfully updated last timestamp to: {timestamp_to_write}")

def process_new_records(last_timestamp):
    """
    Queries BigQuery for new records, processes them,
    and returns the timestamp of the newest processed record.
    """
    table_names = BQ_LINK_TABLES.split(",")
    max_timestamp = None

    for table_name in table_names:
        query = f"""
            SELECT *
            FROM `{GCP_PROJECT}.{BQ_DATASET}.{table_name.strip()}`
            WHERE {BQ_TIMESTAMP_COLUMN} > TIMESTAMP("{last_timestamp}")
            ORDER BY {BQ_TIMESTAMP_COLUMN} ASC
        """
        print(f"Executing query for table {table_name.strip()}:\n{query}")

        query_job = bq_client.query(query)
        rows = list(query_job.result())  # Materialize results to get a count

        if not rows:
            print(f"No new records found in table {table_name.strip()}.")
            continue

        print(f"Found {len(rows)} new records to process in table {table_name.strip()}.")
        max_timestamp_in_batch = None

        # --- Load archive_data_store_config once outside the loop ---
        config_table_query = f"""
            SELECT ar_object, data_store
            FROM `{GCP_PROJECT}.{BQ_DATASET}.archive_data_store_config`
        """
        print(f"Loading config table: {BQ_DATASET}.archive_data_store_config")
        config_job = bq_client.query(config_table_query)
        config_results = list(config_job.result())

        ar_object_to_data_store_map = {
            config_row.get("ar_object"): config_row.get("data_store")
            for config_row in config_results
        }

        print(f"Loaded {len(ar_object_to_data_store_map)} entries from archive_data_store_config.")

        for row in rows:
            # Check if ar_object exists in archive_data_store_config
            ar_object = row.get("ar_object")  # Assuming "ar_object" is the column name

            #Convert BigQuery Row into JSON format and store in variable row_json
            row_json = json.dumps(dict(row), default=str)
            
            if ar_object:
                # Look up data_store from the pre-loaded map
                data_store = ar_object_to_data_store_map.get(ar_object)
                if data_store:
                    print(f"Found data_store: {data_store} for ar_object: {ar_object}")
                else:
                    print(f"No data_store found in config for ar_object: {ar_object}. Skipping GCS object retrieval.")
                    # Skip further processing for this row if no data_store is configured
                    continue # Move to the next row

                arc_doc_id = row.get("arc_doc_id") 

                if arc_doc_id:
                    # Construct the GCS prefix for listing objects in the specified folder
                    # Format: <arc_doc_id>/<arc_doc_id>-
                    gcs_prefix = f"{arc_doc_id}/{arc_doc_id}-"
                    print(f"Attempting to find GCS object with prefix: gs://{GCS_SOURCE_DATA_BUCKET_NAME}/{gcs_prefix}")

                    found_blob = None
                    # List blobs in the bucket with the specified prefix
                    # We use max_results=1 because we only need to find one relevant object.
                    blobs_iterator = storage_client.list_blobs(
                        source_data_bucket,
                        prefix=gcs_prefix,
                        max_results=1
                    )
                    for blob in blobs_iterator:
                        found_blob = blob
                        break # Found the first matching blob, no need to iterate further

                    if found_blob:
                        try:
                            mime_type = found_blob.content_type
                            gcs_uri = f"gs://{GCS_SOURCE_DATA_BUCKET_NAME}/{found_blob.name}"
                            print(f"Successfully downloaded object: {found_blob.name} (MIME type: {mime_type})")

                            # Use Discovery Engine to create a document
                            document_client = discoveryengine.DocumentServiceClient()
                            try:
                                created_document = create_document(document_client, data_store, arc_doc_id, mime_type, row_json, gcs_uri)
                                print(f"Document created: {created_document.id}")
                                print(f"GCS URI: {gcs_uri}")
                            except Exception as e:
                                print(f"Error creating document: {e}")



                        except Exception as e:
                            print(f"Error downloading GCS object '{found_blob.name}': {e}")
                    else:
                        print(f"No GCS object found for arc_doc_id: {arc_doc_id} with prefix '{gcs_prefix}' in bucket '{GCS_SOURCE_DATA_BUCKET_NAME}'.")
                else:
                    print(f"Skipping GCS object retrieval: 'arc_doc_id' column not found in row for ar_object: {ar_object}")
            # For this example, we'll just print the row.
            print(f"Processing row: {dict(row)}")
            # ----------------------------------------

            # Keep track of the latest timestamp in this batch
            current_row_timestamp = row[BQ_TIMESTAMP_COLUMN]
            if max_timestamp_in_batch is None or current_row_timestamp > max_timestamp_in_batch:
                max_timestamp_in_batch = current_row_timestamp

        if max_timestamp_in_batch:
            if max_timestamp is None or max_timestamp_in_batch > max_timestamp:
                max_timestamp = max_timestamp_in_batch

    return max_timestamp


if __name__ == "__main__":
    print("Starting BigQuery processing job...")

    # 1. Get the starting point
    start_timestamp = get_last_timestamp()

    # 2. Query and process new records
    new_high_water_mark = process_new_records(start_timestamp)

    # 3. If we processed anything, update the state for the next run
    if new_high_water_mark:
        write_last_timestamp(new_high_water_mark)
    else:
        print("No updates to the high-water mark needed.")

    print("Job finished successfully.")