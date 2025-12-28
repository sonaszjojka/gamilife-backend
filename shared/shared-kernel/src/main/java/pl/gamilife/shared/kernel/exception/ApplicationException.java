package pl.gamilife.shared.kernel.exception;

public abstract class ApplicationException extends RuntimeException {
    protected ApplicationException(String message) {
        super(message);
    }
}
