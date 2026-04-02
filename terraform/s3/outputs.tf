# Output the bucket name
output "bucket_name" {
  description = "Name of the created S3 bucket"
  value       = aws_s3_bucket.serdp_bucket.bucket
}

output "bucket_arn" {
  description = "ARN of the created S3 bucket"
  value       = aws_s3_bucket.serdp_bucket.arn
}

output "bucket_intelligent_tiering" {
  description = "Is Intelligent Tiering enable?"
  value       = aws_s3_bucket_intelligent_tiering_configuration.serdp_bucket_tiering
}