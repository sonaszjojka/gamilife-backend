package pl.gamilife.user.usecase;

import java.util.UUID;

public interface ResetUserPasswordUseCase {
    void execute(UUID userId, String hashedNewPassword);
}
