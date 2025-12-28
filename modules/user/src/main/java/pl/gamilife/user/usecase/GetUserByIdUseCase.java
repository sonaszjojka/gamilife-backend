package pl.gamilife.user.usecase;

import pl.gamilife.api.user.dto.BasicUserInfoDto;

import java.util.Optional;
import java.util.UUID;

public interface GetUserByIdUseCase {
    Optional<BasicUserInfoDto> execute(UUID userId);
}
