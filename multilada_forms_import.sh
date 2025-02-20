#!/bin/bash

# Assign command-line arguments to variables
BREW_CMD=$1
MYSQL_CMD=$2
MYSQL_USER=$3
MYSQL_DATABASE=$4
MYSQL_DUMPFILE=$5

echo "Restoring database '$MYSQL_DATABASE' from file '$MYSQL_DUMPFILE'..."

# Restart MySQL service
echo "Restarting MySQL..."
$BREW_CMD services restart mysql

# Wait for MySQL to be ready
echo "Waiting for MySQL to start..."
until $MYSQL_CMD -u $MYSQL_USER -e "SELECT 1;" &> /dev/null; do
    sleep 2
    echo "Still waiting..."
done
echo "MySQL is ready."

# Disable strict mode
$MYSQL_CMD -u $MYSQL_USER -e "SET GLOBAL sql_mode = '';"

# Import the database
$MYSQL_CMD -u $MYSQL_USER $MYSQL_DATABASE < $MYSQL_DUMPFILE

# Final confirmation message
echo "Backup of database '$MYSQL_DATABASE' from file '$MYSQL_DUMPFILE' has been successfully restored locally"

