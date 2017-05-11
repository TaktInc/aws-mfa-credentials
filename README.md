aws-mfa-credentials
====================

Keep your AWS credentials file up to date with MFA-carrying temporary credentials

Usage
------

`aws-mfa-credentials --mfa-serial-number SERIAL_NUMBER`

**See [Locking protcol](#locking-protocol) for important information about avoiding data loss!**

From the [API reference for GetSessionToken][GetSessionToken]:

> SerialNumber: The identification number of the MFA device that is associated with the IAM user who is making the GetSessionToken call. Specify this value if the IAM user has a policy that requires MFA authentication. The value is either the serial number for a hardware device (such as GAHT12345678) or an Amazon Resource Name (ARN) for a virtual device (such as arn:aws:iam::123456789012:mfa/user). You can find the device for an IAM user by going to the AWS Management Console and viewing the user's security credentials.

Optional flags:
  * `--duration DURATION_SECONDS` The lifetime of the credentials. Defaults to AWS default (43200 at the time of this writing).
  * `--profile PROFILE` The name of the profile whose credentials we're managing. Defaults to 'default'.
  * `--refresh-lead-time LEAD_SECONDS` The amount of time before credential expiry to refresh the credentials. Defaults to 300.

Environment variables:
  * `PATH`: A program named `ssh-askpass`, accepting a prompt as an argument and outputting a password to stdout, must be available on the path.
  * `HOME`: The home directory containing the `.aws/credentials` file

Operation
-----------

`aws-mfa-credentials` will request an MFA code from the user using `ssh-askpass`, request temporary credentials from Amazon, and update the credentials file with the temporary credentials.

The new credentials are placed into a profile named `PROFILE-mfa`, e.g. when the `--profile` flag is not given they are placed in `default-mfa`. Use the `AWS_PROFILE` environment variable or other profile-specifying mechanisms to use the credentials.

`aws-mfa-credentials` exits on any error, you may want to run it in a retry loop (with backoff).

Locking protocol
------------------

To allow safe usage of multiple instances of `aws-mfa-credentials` in parallel, a locking protocol is used for the credentials file: An exclusive fcntl lock is held on the file `$HOME/.aws/.credentials.lock` when writing (and thus data read before the lock is obtained must be rechecked within the lock).

Unfortunately, the AWS cli and other tools do not respect this protocol. Therefore, if you are modifying the credentials file in any way besides `aws-mfa-credentials`, you should either
stop any existing instances of `aws-mfa-credentials` or obtain the lock yourself. **Credential loss may occur if this is not followed!**

[GetSessionToken]: http://docs.aws.amazon.com/STS/latest/APIReference/API_GetSessionToken.html
