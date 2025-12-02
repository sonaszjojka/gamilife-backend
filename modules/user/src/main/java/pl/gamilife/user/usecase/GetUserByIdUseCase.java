package pl.gamilife.user.usecase;

import pl.gamilife.api.user.dto.BasicUserInfoApiDto;

import java.util.Optional;
import java.util.UUID;

public interface GetUserByIdUseCase {
    Optional<BasicUserInfoApiDto> execute(UUID userId);
}
