# hs-editorial-cms
Lightweight Editorial Content Management System for journals and magazines

# Create PostgreSQL database (ubuntu)
```
sudo su postgres -c 'createuser --createdb --encrypted --no-inherit --login --pwprompt --no-createrole --no-superuser --no-replication editorialcms'
sudo su postgres -c 'createdb --encoding=UTF-8 --owner=my-project --template=template0 editorialcms'
```

# Run Yesod server
```
cd hs-editorialcms
sh run_dev.sh
```
# Initialize admin user
only needed after the first start
```
curl http://localhost:3000/initdb/me@example.com
```

you can see the generated password in the yesod logs like
```
[Error] ###############################
[Error] admin login:         admin
[Error] admin password:      XpISNqkWAGFfrlryWxNzbtkvJzFkszVj
[Error] admin password hash: sha256|17|CPlYqS5DBuHSZRtbJjUasQ==|wptBAbVHgSSRnJJ+7X1P6PbsJcM+oZ+JaGjs1xVNJns=
[Error] admin email:         me@example.com
[Error] ###############################
```
thus the generated password of the 'admin' user in this sample is: XpISNqkWAGFfrlryWxNzbtkvJzFkszVj

# Generate new password hashes
normally you set the password on the browser while the server is running
but in case you have to reset them hard in the database
```
sh run_passwd.sh mySecretPassword78574
```

will generate the hash: sha256|17|sm/d6UdH7+nRXZt7bJaAeg==|HVpHzSBOHQPwZiqmfEHiCZZPZh4tFLQyEuDKsNdqcN0=

then you can update the apprpriate database row for the admin user or some other user

All model definitions and the ginger templates are in the hs-generator subproject.
If model definitions changed you can regenerate the haskell code with
```
cd my-project
sh run_gen.sh
```
