library(rJava)

# Initialize JVM
.jinit()

# Set classpath (update to the correct path)
.jaddClassPath(".")

# Load Java class
helloClass <- .jnew("HelloWorld")

# Call the Java method
result <- .jcall(helloClass, "S", "greet", "Ananya")
print(result)
