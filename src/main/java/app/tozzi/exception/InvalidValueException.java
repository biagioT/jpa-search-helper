package app.tozzi.exception;

import lombok.Getter;


@Getter
public class InvalidValueException extends JPASearchException {

    private final String field;
    private final Object value;

    public InvalidValueException(String message, String field, Object value) {
        super(message);
        this.field = field;
        this.value = value;
    }

    public InvalidValueException(String message, Throwable cause, String field, Object value) {
        super(message, cause);
        this.field = field;
        this.value = value;
    }

}
