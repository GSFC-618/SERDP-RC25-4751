variable "region" {
  description = "AWS region"
  type        = string
  default     = "us-west-2"
}

variable "bucket_name" {
  description = "Name of the S3 bucket"
  type        = string
  default     = "serdp-sandbox"
}

variable "environment" {
  description = "Environment name"
  type        = string
  default     = "development"
}

variable "project" {
  description = "Project name"
  type        = string
  default     = "SERDP-RC25-4751"
}

variable "owner" {
  description = "Owner/Team name"
  type        = string
  default     = "NASACode618"
}

variable "purpose" {
  description = "Primary use of the s3 bucket"
  type        = string
  default     = "DataStorage"
}

variable "enable_intelligent_tiering" {
  description = "Enable S3 Intelligent Tiering"
  type        = bool
  default     = false
}
