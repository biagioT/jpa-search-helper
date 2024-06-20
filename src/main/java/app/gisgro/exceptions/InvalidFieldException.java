package app.gisgro.exceptions;

import lombok.Getter;


@Getter
public class InvalidFieldException extends RuntimeException {

    private final String field;

    public InvalidFieldException(String message, String field) {
        super(message);
        this.field = field;
    }
}
