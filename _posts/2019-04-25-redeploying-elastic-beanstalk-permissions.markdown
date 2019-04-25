---
layout: post
title: "IAM Permissions for Deploying an Elastic Beanstalk version from the CLI"
date: 2019-04-25 16:15:00 -0800
categories: aws
---

I really wanted to automate redeploying an Elastic Beanstalk
environment using a custom [Docker
image](https://github.com/metabase/docker-ci-build-image) running on
CircleCI. It actually took a little more work than I originally
expected. Here's the stuff that I couldn't find anywhere else on the
Internet that I eventually figured out thru trial and error.

You can read all about how to install the Elastic Beanstalk CLI in the
[AWS
docs](https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/eb-cli3-install.html#eb-cli3-install.cli-only)
so I won't repeat that here. What the docs don't really cover is what
sort of IAM permissions you're going to need to use various commands provided by the CLI.

It really took a lot of trial and error to figure this out but I was finally able to run

```bash
# (Re)deploy $APPLICATION_VERSION
eb deploy --verbose --nohang --version $APPLICATION_VERSION $EB_ENVIRONMENT
```

#### IAM Security Policy with Permissions to Deploy Elastic Beanstalk Version from CLI

Using an IAM user to which I attached a security policy with the following permissions:

```json
{
    "Version": "2012-10-17",
    "Statement": [
        {
            "Effect": "Allow",
            "Action": [
                "autoscaling:*",
                "cloudformation:DescribeStackResource",
                "cloudformation:DescribeStackResources",
                "cloudformation:GetTemplate",
                "cloudfront:CreateInvalidation",
                "elasticbeanstalk:CreateApplicationVersion",
                "elasticbeanstalk:DescribeApplications",
                "elasticbeanstalk:DescribeApplicationVersions",
                "elasticbeanstalk:DescribeConfigurationSettings",
                "elasticbeanstalk:DescribeEnvironments",
                "elasticbeanstalk:UpdateEnvironment",
                "elasticloadbalancing:DeregisterTargets",
                "elasticloadbalancing:DescribeTargetGroups",
                "elasticloadbalancing:DescribeTargetHealth",
                "elasticloadbalancing:RegisterTargets",
                "s3:ListAllMyBuckets"
            ],
            "Resource": "*"
        },
        {
            "Effect": "Allow",
            "Action": [
                "s3:PutObject",
                "s3:GetObjectAcl",
                "s3:GetObject",
                "s3:CreateBucket",
                "s3:ListBucket",
                "s3:DeleteObject",
                "s3:GetBucketPolicy",
                "s3:PutObjectAcl"
            ],
            "Resource": [
                "arn:aws:s3:::elasticbeanstalk-us-east-1-111122223333",
                "arn:aws:s3:::elasticbeanstalk-us-east-1-111122223333/*"
            ]
        }
    ]
}
```

Replace the `111122223333` in the name of the S3 buckets with your own
AWS account ID and `us-east-1` with the region your Elastic Beanstalk
application is in. You should be able to find this
`elastic-beanstalk-` bucket by looking in S3.

You could probably down these permissions even further by figuring out
exactly what `autoscaling:` permissions you need and restricting the
resources a bit further but I didn't have all day to do that so that
is left as an exercise to the reader.

#### Elastic Beanstalk CLI Configuration File

If you try to run `eb deploy` for the first time it will complain and
ask you to run `eb init` to interactively configure it first. Not a
good thing when we're trying to automate the whole process.

All this tool does is generate a file called
`.elasticbeanstalk/config.yml` in your current directory and fill it
with a bunch of properties.

Bonus tip: you don't need to run `eb init` at all. You can create
`.elasticbeanstalk/config.yml` yourself, and thru some trial and error
I found you only need to put the following things in it:

```yaml
global:
  application_name: your-eb-application-name
  default_region: us-east-1
```

Change those as appropriate. You still need to pass credentials for
your IAM user with the policy above, but as with other AWS CLI tools
you can pass them by setting `AWS_ACCESS_KEY_ID` and
`AWS_SECRET_ACCESS_KEY`.
