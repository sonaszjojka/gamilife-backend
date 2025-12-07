package pl.gamilife.gamification.domain.exception;

import pl.gamilife.shared.kernel.exception.DomainException;

public class InvalidGamificationOperationException extends DomainException {
    public InvalidGamificationOperationException(String message) {
        super(GamificationErrorCode.INVALID_GAMIFICATION_OPERATION, message);
    }
}
