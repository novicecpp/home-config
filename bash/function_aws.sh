# sort ip by https://www.cyberciti.biz/faq/unix-linux-shell-script-sorting-ip-addresses/
f_aws_list_ip_by_tag_name() {
    aws ec2 describe-instances --filters Name=tag:Name,Values=$1 --query 'Reservations[].Instances[].[PrivateIpAddress]' --output text  | sort -t . -k 3,3n -k 4,4n
}
