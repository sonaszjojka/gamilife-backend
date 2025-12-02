package pl.gamilife.user.usecase;

import pl.gamilife.api.user.dto.CheckIfUsersEmailIsVerifiedApiDto;

import java.util.UUID;

public interface CheckIfUsersEmailIsVerifiedUseCase {
    CheckIfUsersEmailIsVerifiedApiDto execute(UUID userId);
}
