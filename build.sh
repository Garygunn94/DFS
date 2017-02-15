#!/bin/sh
loc="$PWD"

cd "$loc/CommonResources"
stack setup
stack build
echo "Building Directory Server..."
cd "$loc/DirectoryServer"
stack setup
stack build
echo "Building File Server..."
cd  "$loc/FileServer"
stack setup
stack build
echo "Building Authentication Server..."
cd  "$loc/AuthServer"
stack setup
stack build
echo "Building Locking Service..."
cd  "$loc/LockingService"
stack setup
stack build
echo "Building Client Proxy..."
cd  "$loc/ClientProxy"
stack setup
stack build
echo "Building Transaction Server..."
cd  "$loc/TransactionServer"
stack setup
stack build