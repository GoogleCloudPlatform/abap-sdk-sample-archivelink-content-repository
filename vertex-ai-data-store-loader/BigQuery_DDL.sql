-- Inserting a record for Financial Accounting Documents
CREATE TABLE `<your-google-cloud-project>.<your-bigquery-dataset>.archive_data_store_config`
(
  ar_object STRING OPTIONS(description="Archive Object"),
  data_store STRING OPTIONS(description="Vertex AI Data Store ID")
);

-- Inserting a record for each archive object for which data shoould be indexed in Vertex AI Data Store. 
-- You can find Archive Objects from table TOADV in SAP. This example uses EDXORDPDF (Purchase Order PDF)
INSERT INTO `<your-google-cloud-project>.<your-bigquery-dataset>.archive_data_store_config` (ar_object, data_store)
VALUES('EDXORDPDF', '<Vertex AI Data Store ID>');