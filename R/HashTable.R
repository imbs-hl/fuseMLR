#' @title Class HashTable
#' @description Hashtable to contain object entities. Study and layers are extensions of this class.
#'
#' @export
#' @importFrom R6 R6Class
#'
#' @importFrom digest digest
HashTable <- R6Class("HashTable",
                     public = list(
                       #' @description
                       #' Initialize a default parameters list.
                       #'
                       #' @param id (`character(1)`)\cr
                       #'  ID of the hash table. It must be unique.
                       #'
                       #' @export
                       initialize = function (id) {
                         private$id = id
                         private$key_class = data.frame(key = character(0L),
                                                        class = character(0L))
                         private$hash_table = new.env(hash = TRUE,
                                                      parent = emptyenv())
                       },
                       #' @description
                       #' Function to add a key-value pair to the hash table.
                       #' @param key (`character(1)`)\cr
                       #' The key to be added.
                       #' @param value (`object(1)`)\cr
                       #' Object to be added.
                       #' @param .class (`character(1)`)\cr
                       #' Class of the object to be added.
                       #' @export
                       #'
                       add2HashTable = function (key, value, .class) {
                         hash_key = digest(key, algo = "xxhash64")
                         private$hash_table[[hash_key]] = value
                         # Tests if the instance has already been stored and
                         # updates table accordingly.
                         if (!nrow(self$getKeyClass())) {
                           private$key_class[nrow(private$key_class) + 1L, ] = c(key,
                                                                                 .class)
                         } else {
                           key_exists = (key %in% self$getKeyClass()$key)
                           class_exists = (.class %in% self$getKeyClass()$class)
                           if (!(key_exists & class_exists)) {
                             private$key_class[nrow(private$key_class) + 1L, ] = c(key,
                                                                                   .class)
                           }
                         }
                         invisible(private)
                       },
                       #' @description
                       #' Getter of the object which the key passed as argument.
                       #'
                       #' @param key `character()` \cr
                       #' Key of the required object.
                       #'
                       #' @export
                       #'
                       getFromHashTable = function (key) {
                         hash_key = digest(key, algo = "xxhash64")
                         if (exists(hash_key, envir = private$hash_table)) {
                           return(private$hash_table[[hash_key]])
                         } else {
                           return(NULL)
                         }
                       },
                       #' @description
                       #' Getter of the \code{data.frame} that stores all key class pairs.
                       #'
                       #' @return
                       #' [data.frame]
                       #'
                       #' @export
                       #'
                       getKeyClass = function () {
                         return(private$key_class)
                       },
                       #' @description
                       #' Remove the object with the corresponding key from the hashtable.
                       #'
                       #' @param key
                       #' Key of the object to be removed.
                       #'
                       #' @export
                       #'
                       removeFromHashTable = function (key) {
                         hash_key = digest(key, algo = "xxhash64")
                         if (exists(hash_key, envir = private$hash_table)) {
                           rm(list = hash_key, envir = private$hash_table)
                           rm_index = which(private$key_class$key == key)
                           private$key_class = private$key_class[- rm_index , ]
                         }
                         invisible(TRUE)
                       },
                       #' @description
                       #' Printer
                       #' @param ... (any) \cr
                       #'
                       #' @export
                       #'
                       print = function (...) {
                         cat("Class: HashTable\n")
                         cat(sprintf("id: %s\n", private$id))
                         cat("-----------------\n")
                         print(private$key_class)
                       },
                       #' @description
                       #' Getter of the current object ID.
                       #'
                       #' @export
                       #'
                       getId = function () {
                         return(private$id)
                       },
                       #' @description
                       #' Getter of the current hashtable.
                       #'
                       #' @export
                       getHashTable = function () {
                         return(private$hash_table)
                       },
                       #' @description
                       #' Check whether object from a class has already been stored.
                       #'
                       #' @param .class `character()` \cr
                       #'
                       #' @return
                       #' Boolean value
                       #'
                       checkClassExist = function (.class) {
                         return(.class %in% private$key_class$class)
                       }
                     ),
                     private = list(
                       id = character(0L),
                       hash_table = NULL,
                       key_class = NULL
                     ),
                     # TODO: define a deep_clone function for this class.
                     cloneable = FALSE
)
