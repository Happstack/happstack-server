How to run the distributed chat:
  1. Compile two or more instances of the application
      ghc --make DistributedChat.hs -o chat1
      ghc --make DistributedChat.hs -o chat2
  2. Make sure the spread daemon is running.
      /etc/init.d/spread start  OR
      run spread manually
  3. Execute the instances
      ./chat1 8000
      ./chat2 8001
