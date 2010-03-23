import time

last_called = {}

def true_at_most_every_n_seconds( hash, reportdelay):
    #is this the first call?
    now = time.time()
    if hash not in last_called.keys():
        last_called[hash]=now
        return True
    else:
        if now - last_called[hash] >= reportdelay:
            last_called[hash] = now
            return True

    return False





def demo():


    def myreportingfunction():
        if nsim.reporttools.true_at_most_every_n_seconds('my',2):
            print "This is a report at %s" % time.asctime()
    
    import nsim
    import time
    for i in range(20):
        time.sleep(0.5)
        print "ping"
        myreportingfunction()

if __name__ == "__main__":
    demo()


    
