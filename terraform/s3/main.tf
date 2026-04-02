# Create a new s3 bucket for the project

# TerraFrom code
terraform {
  required_version = ">=1.5.7"
}
provider "aws" {
  region = var.region
}

# Define the S3 bucket resource
# Create S3 bucket with dynamic tags
resource "aws_s3_bucket" "serdp_bucket" {
  bucket = var.bucket_name

  tags = {
    Name        = var.bucket_name
    Environment = var.environment
    Project     = var.project
    Owner       = var.owner
    Purpose     = var.purpose
    CreatedBy   = "Terraform"
    CreatedDate = formatdate("YYYY-MM-DD", timestamp())
  }
}

# Intelligent Tiering (conditional)
resource "aws_s3_bucket_intelligent_tiering_configuration" "serdp_bucket_tiering" {
  count  = var.enable_intelligent_tiering ? 1 : 0
  bucket = aws_s3_bucket.serdp_bucket.id
  name   = "EntireBucket"

  status = "Enabled"

  tiering {
    access_tier = "DEEP_ARCHIVE_ACCESS"
    days        = 180
  }

  tiering {
    access_tier = "ARCHIVE_ACCESS"
    days        = 125
  }
}

#resource "aws_s3_bucket_intelligent_tiering_configuration" "my_tiering_config" {
#  bucket = aws_s3_bucket.my_intelligent_tiering_bucket.id
#  name   = "MyIntelligentTieringConfiguration"
#  status = "Enabled" # Can also be "Disabled"
#
#  filter {
#    prefix = "data/" # Optional: Apply to objects with this prefix
#    tags = { # Optional: Apply to objects with these tags
#      "environment" = "production"
#      "project"     = "myproject"
#    }
#  }
#
#  tiering {
#    access_tier = "ARCHIVE_ACCESS"
#    days        = 90 # Transition to ARCHIVE_ACCESS after 90 days of no access
#  }
#
#  tiering {
#    access_tier = "DEEP_ARCHIVE_ACCESS"
#    days        = 180 # Transition to DEEP_ARCHIVE_ACCESS after 180 days of no access
#  }
#}
