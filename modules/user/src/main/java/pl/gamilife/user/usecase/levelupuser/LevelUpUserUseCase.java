package pl.gamilife.user.usecase.levelupuser;

import pl.gamilife.api.user.dto.BasicUserInfoDto;
import pl.gamilife.shared.kernel.architecture.UseCase;

public interface LevelUpUserUseCase extends UseCase<LevelUpUserCommand, BasicUserInfoDto> {
}
