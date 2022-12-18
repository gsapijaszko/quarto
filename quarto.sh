zip -r site.zip _site/
scp -o HostKeyAlgorithms\ ssh-rsa site.zip sapi@www.sapijaszko.net://home/sapi
luit -encoding ISO-8859-2 ssh -o HostKeyAlgorithms\ ssh-rsa dobrazmiana.sapijaszko.net
