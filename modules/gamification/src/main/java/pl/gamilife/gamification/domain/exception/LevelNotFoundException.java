package pl.gamilife.gamification.domain.exception;

import pl.gamilife.shared.kernel.exception.DomainException;

public class LevelNotFoundException extends DomainException {
    public LevelNotFoundException(String message) {
        super(GamificationErrorCode.LEVEL_NOT_FOUND, message);
    }
}
