package pl.gamilife.user.usecase;

import pl.gamilife.api.user.dto.CheckIfUsersEmailIsVerifiedDto;

import java.util.UUID;

public interface CheckIfUsersEmailIsVerifiedUseCase {
    CheckIfUsersEmailIsVerifiedDto execute(UUID userId);
}
