# Very opinionated, small library, to create static websites.

###### \[in package S-BLOG\]
## s-blog ASDF System Details

- Version: 0.0.2
- Description: s-blog helps creating static websites, possibly hosted on Amazon S3
- Licence: The MIT License (MIT)
- Author: Crackbot <thecrackbot@gmail.com>

## s-blog API

- [macro] DEFBLOG &KEY HOST (STATIC-PATHS 'NIL) S3-CONFIG (SYNC-ON-COMPILE)

    Define new s-blog. Each blog is tied to defining package, meaning
    that you can have only one blog per package.

- [macro] DEFPAGE NAME &BODY BODY

    Define new page inside the blog.
    
    Page name is later translated into url which is used to access it on
    the web. For example page named as /test/page will be translated to
    either test/page.html or test/page/index.html depending on :no-prefix
    setting.

## Publish your files via Amazon S3.

- [function] S3-SYNC-PAGE BLOG PAGE S3-CONFIG

    Sync page to s3.

- [function] S3-SYNC-PATH PATH S3-CONFIG

    Sync path to s3.

## Blacklist static files

Some files you don't want to get processed and put out there. For
cases like this there is a blacklist feature, which is a list of
functions if any of this functions returns t then sync process will
ignore the file.

- [variable] *FILENAME-BL* (#<FUNCTION (LAMBDA (FN NAME)
              :IN
              "/home/cb/dev/2015/s-blog/src/main.lisp")
   {100DDDE55B}>)

    Variable holding the list of blacklist functions.

- [function] BLACKLISTED-P FILENAME

    Checks if filename is blacklisted or not, also see *filename-bl*

## Low level

- [variable] *BLOGS* #<HASH-TABLE :TEST EQL :COUNT 0 {100DDC8003}>

    Global variable holding the package -> blog mapping

- [class] BLOG

- [reader] NO-POSTFIX BLOG

    If set to t will create a path named as page name
    and place page source as index.html. This is done to have nicer
    URLs in browser, for example instead of /test.html you will get
    /test/

- [accessor] HOST BLOG

    Host where you can access the blog.

- [accessor] STATIC-PATHS BLOG

    Static paths holds different static files that
    needs to be synced, javascript or css for example, for some files
    types there is also preprocessing that will happen.

- [accessor] S3-CONFIG BLOG

    S3 config is a plist with :access-key :secret-key
    props. Optionally you can specify :bucket, otherwise it will be
    determined from hostname, which may not always produce the results
    that you want. You need to specify this if you want to publish
    your blog through amazon s3.
