'OSC uses python 2.7

import OSC
import time, random
client=OSC.OSCClient()
client.connect(('127.0.0.1',57120))
msg=OSC.OSCMessage()
msg.setAddress("/print")
msg.append(500)
client.send(msg)