package app.tozzi.exception;

import lombok.Getter;


@Getter
public class JPASearchException extends RuntimeException {

    public JPASearchException(String reason) {
        super(reason);
    }

    public JPASearchException(Throwable cause) {
        super(cause);
    }

    public JPASearchException(String reason, Throwable cause) {
        super(reason, cause);
    }

}
