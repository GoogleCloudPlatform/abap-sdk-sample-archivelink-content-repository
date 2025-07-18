# SAP Content Repository Integration with Vertex AI Data Store & Agentspace
Using this guide you can implement periodic synchronization of Attachments Stored in [Google Cloud Storage Content Repository Solution](https://cloud.google.com/solutions/sap/docs/abap-sdk/on-premises-or-any-cloud/latest/implement-gcs-sap-content-repository) into [Vertex AI Data Store](https://cloud.google.com/generative-ai-app-builder/docs/create-datastore-ingest) and [Google Agentspace](https://cloud.google.com/products/agentspace?hl=en)

## Prerequisites
1.  **Google Cloud SDK (`gcloud`)** installed and configured.
3.  A **Google Cloud Project** with the following APIs enabled:
    * Cloud Run API
    * Cloud Build API
    * Artifact Registry API
    * BigQuery API
    * Cloud Storage API

## SAP Setup
For this solution to work, you must implement Google Cloud Storage Content Repository Solution in your SAP system. 

1. Download, install & configure latest version of [ABAP SDK for Google Cloud](https://cloud.google.com/solutions/sap/docs/abap-sdk/on-premises-or-any-cloud/latest/install-config)
2. Import ABAP source code of this [Github repo](https://github.com/GoogleCloudPlatform/abap-sdk-sample-archivelink-content-repository/tree/main/src) via abapgit or [Transport Request from Release](https://github.com/GoogleCloudPlatform/abap-sdk-sample-archivelink-content-repository/releases)
3. Configure Content Repository Solution. You have 2 choices - 
  * Follow the official [guide here](https://cloud.google.com/solutions/sap/docs/abap-sdk/on-premises-or-any-cloud/latest/implement-gcs-sap-content-repository), Or
  * Alternatively, you can also implement the open source version of the solution included in this [Github Repo itself](https://github.com/GoogleCloudPlatform/abap-sdk-sample-archivelink-content-repository/?tab=readme-ov-file#steps-to-setup-archive-link-for-cloud-storage). 

  Important Note - Github version is provided as a sample and is not officially supported by Google. 
  However, you have the flexibility to modify the Github solution as per your requirements. 

4. Configure [BigQuery Toolkit](https://cloud.google.com/solutions/sap/docs/abap-sdk/on-premises-or-any-cloud/latest/bq-toolkit-for-sap-overview) for ArchiveLink Tables TOA01, TOA02, TOA03 etc. You can get Link tables from Tcode OAC3.
5. Configure entries in table ZGOOG_ALINK_BQ with [Content Repository](https://cloud.google.com/solutions/sap/docs/abap-sdk/on-premises-or-any-cloud/latest/implement-gcs-sap-content-repository#configure-content-repository-for-gcs) Name and [BigQuery Toolkit Mass Transfer Key](https://cloud.google.com/solutions/sap/docs/abap-sdk/on-premises-or-any-cloud/latest/bq-toolkit-for-sap-configuration#configure-connection)

When you save an attachment in SAP, the implementation of BAPI OA_BADI_LINK will asynchronously insert the ArchiveLink entry saved in TOA01/2/3 tables into corresponding tables in BigQuery. These entries will be used by the Cloud Run to synchronize Vertex AI Data store 

## BigQuery Setup
Run SQL statements from [BigQuery_DDL.sql](./BigQuery_DDL.sql) in your [BigQuery Console](https://console.cloud.google.com/bigquery).
This will setup BigQuery Configurations required for this solution

## Google Cloud Storage for Timestamp Handling
Create a [Cloud Storage Bucket](https://console.cloud.google.com/storage) ```your-bucket-name-for-timestamp-file``` and add a text file ```last_timestamp.txt``` with value single value 1971-01-01T00:00:00Z in it. This file will be used by the Cloud Run Job to handle change data capture on new attachments stored

## Create a Vertex AI Data Store 
Run this command in your terminal to create a Vertex AI Data Store for Archivelink documents

```
curl -X POST \
-H "Authorization: Bearer $(gcloud auth print-access-token)" \
-H "Content-Type: application/json" \
-H "X-Goog-User-Project: <your-google-cloud-project-id>" \
"https://discoveryengine.googleapis.com/v1alpha/projects/<your-google-cloud-project-id>/locations/global/collections/default_collection/dataStores?dataStoreId=<your-data-store-id>" \
-d '{
  "displayName": "SAP ArchiveLink Documents",
  "industryVertical": "GENERIC",
  "solutionTypes": ["SOLUTION_TYPE_SEARCH"]
  "contentConfig": "CONTENT_REQUIRED"
}'
```

## Create and deploy Cloud Run Job for periodic sync of new attachments to Vertex AI Data Store

Before running the commands make sure you are in the directory or folder where this README.md file exists.

Set these variables in your terminal where gcloud is installed and configured. They will be used in the deployment commands.

```
export GCP_PROJECT="your-google-cloud-project" #Must be replaced with your project
export REGION="google-cloud-region"  #Example - us-central1
export REPO_NAME="content-repository-indexer-repo"
export IMAGE_NAME="content-repository-indexer-image"
export IMAGE_TAG="latest"
export SERVICE_NAME="sap-content-repository-indexing-job"
```

```
export GCS_TIMESTAMP_BUCKET_NAME="your-bucket-name-for-timestamp-file"  #Must be replaced with your bucket name created above
export GCS_STATE_FILE="last_timestamp.txt"
export GCS_SOURCE_DATA_BUCKET_NAME="your-bucket-name-for-source-data" #Must be replaced with your bucket name configured in Content Repository Solution in SAP
export BQ_DATASET="your-bigquery-dataset" #Must be replaced with your dataset
export BQ_LINK_TABLES="toa01,toa02,toa03" #The Link table/s from SAP Tcode OAC3
export BQ_TIMESTAMP_COLUMN="recordstamp"
export DISCOVERY_ENGINE_DATA_STORE_ID="sap-attachment-data-store"
```

```
export IMAGE_PATH="${REGION}-docker.pkg.dev/${GCP_PROJECT}/${REPO_NAME}/${IMAGE_NAME}:${IMAGE_TAG}"
```

Build image for Cloud Run Job by running the command in your terminal

```
gcloud builds submit --tag "${IMAGE_PATH}" .
echo "Docker image built and pushed to Artifact Registry: ${IMAGE_PATH}"
```

Create Cloud Run Job by running the command in your terminal

```
gcloud run jobs deploy "${SERVICE_NAME}" \
  --image "${IMAGE_PATH}" \
  --region $REGION \
  --parallelism=1 \
  --set-env-vars="GCP_PROJECT=$GCP_PROJECT" \
  --set-env-vars="GCS_TIMESTAMP_BUCKET_NAME=$GCS_TIMESTAMP_BUCKET_NAME" \
  --set-env-vars="GCS_SOURCE_DATA_BUCKET_NAME=$GCS_SOURCE_DATA_BUCKET_NAME" \
  --set-env-vars="GCS_STATE_FILE=$GCS_STATE_FILE" \
  --set-env-vars="BQ_DATASET=$BQ_DATASET" \
  --set-env-vars="BQ_LINK_TABLES=$BQ_LINK_TABLES" \
  --set-env-vars="BQ_TIMESTAMP_COLUMN=$BQ_TIMESTAMP_COLUMN" \
  --set-env-vars="DISCOVERY_ENGINE_DATA_STORE_ID=$DISCOVERY_ENGINE_DATA_STORE_ID"
```  

## Schedule Cloud Run Job

Follow the [instruction](https://cloud.google.com/run/docs/execute/jobs-on-schedule#using-scheduler) to schedule this job to run on a periodic basis.

This Cloud Run Job will -
1) Find all newly inserted documents IDs from Archive Link tables in BigQuery
2) Extract corresponding URI and Metadata of these documents from Google Cloud Storage Bucket
3) Send these documents to Vertex AI Data Store for indexing

## Link Datastore to Agentspace
1) Go to [Google Cloud Agentspace](https://console.cloud.google.com/agentspace/)
2) Click Manage on the Agentspace tile
3) Select your Agentspace App from the list of Apps
4) On the left navigation page, go to Connected data stores
5) Click Add existing data stores and select the data store name you created above

After linking, newly added documents will be available for Search through Agentspace App
