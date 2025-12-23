package pl.gamilife.user.usecase;

import java.util.UUID;

public interface UpdateUserPasswordUseCase {
    void execute(UUID userId, String hashedNewPassword);
}
