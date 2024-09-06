
# How I generated private-key-traditional.pem:

openssl genrsa -out private-key.pem 3072

https://stackoverflow.com/questions/2957742/how-to-convert-pkcs8-formatted-pem-private-key-to-the-traditional-format:

openssl pkey -in private-key.pem -traditional > private-key-traditional.pem
