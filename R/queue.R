queue <- function() {
        queue <- list()
        structure(function(action, ...) {
                          peek <- if (length(queue)) queue[[length(queue)]] else NULL
                          queue <<- action(queue, ...)
                          peek},
                  class="queue")
}
dequeue <- function(x, ...) UseMethod("dequeue")
dequeue.list <- function(x, ...) if (length(x)) x[-length(x)] else x
dequeue.queue <- function(x, ...) x(dequeue)
enqueue <- function(x, y, ...) UseMethod("enqueue")
enqueue.list <- function(x, y, ...) c(list(y), x)
enqueue.queue <- function(x, y, ...) x(enqueue, y)

as.list.queue <- function(x, ...) get("queue", environment(x))
format.queue <- function(x, ...) c("Queue:", length(as.list(x)), "Elements")
print.queue <- function(x, ...) cat(format(x), "\n")
str.queue <- function(object, ...) {
        l <- as.list(object)
        print(object)
        if (length(l))
                cat(c("[ Back to Front ]"), capture.output(str(l))[-1], sep="\n")
}
