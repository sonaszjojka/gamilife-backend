package pl.gamilife.user.usecase;

import pl.gamilife.api.user.dto.BasicUserInfoDto;

import java.util.UUID;

public interface ConfirmUserEmailVerificationUseCase {
    BasicUserInfoDto execute(UUID userId);
}
