import nsim
import time






def test_calling_too_fast():
    #register the first call
    nsim.reporttools.true_at_most_every_n_seconds('my',20)
    #expect this to be false as we call the second time faster than 20 seconds ago
    assert nsim.reporttools.true_at_most_every_n_seconds('my',20) == False


def test_calling_too_slow():
    #register the first call
    nsim.reporttools.true_at_most_every_n_seconds('my',0.5)
    #expect this to be false as we call the second time faster than 20 seconds ago
    time.sleep(1)
    assert nsim.reporttools.true_at_most_every_n_seconds('my',0.5) == True


def test_change_interval():
    nsim.reporttools.true_at_most_every_n_seconds('my',100)
    time.sleep(1)
    assert nsim.reporttools.true_at_most_every_n_seconds('my',100) == False
    assert nsim.reporttools.true_at_most_every_n_seconds('my',0) == True





