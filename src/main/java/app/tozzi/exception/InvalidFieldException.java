package app.tozzi.exception;

import lombok.Getter;


@Getter
public class InvalidFieldException extends JPASearchException {

    private final String field;

    public InvalidFieldException(String message, String field) {
        super(message);
        this.field = field;
    }

    public InvalidFieldException(String message, Throwable cause, String field) {
        super(message, cause);
        this.field = field;
    }

}
