package pl.gamilife.user.usecase;

import pl.gamilife.api.user.dto.BasicUserInfoApiDto;

import java.util.UUID;

public interface ConfirmUserEmailVerificationUseCase {
    BasicUserInfoApiDto execute(UUID userId);
}
