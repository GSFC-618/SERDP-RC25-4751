### Quick Steps for Deploying with TerraForm

1. First authenticate with AWS using NASA SSO via `aws-sso login` and follow the prompts to authenticate
2. Select a profile to use for creating the buckets in AWS via `aws-sso-profile <profile_name>` (usually defeined in ~/.aws/config)
3. Modify or use the existing terraform code for creating a new s3
4. Run `terraform init` to initialize the project.
5. Run `terraform plan -out tfplan` to check the config and create the plan (stored in a binary file called tfplan )
Options: You can customize s3 variables at the plan stage. E.g. `terraform plan -var="bucket_name=serdp-sandbox" -out tfplan`

To create the bucket in production w/o intelligent tiering (can be added later), e.g.

`terraform plan -var="bucket_name=serdp-data" -var="enable_intelligent_tiering=false" -var="environment=production" -out tfplan`

6. Run `terraform apply tfplan` to apply that exact plan.

7. If you are reusing the terraform with dynamic variables to create multiple s3 buckets you need to delete the created terraform plan and state files to make sure you dont delete the bucket just create a new one
