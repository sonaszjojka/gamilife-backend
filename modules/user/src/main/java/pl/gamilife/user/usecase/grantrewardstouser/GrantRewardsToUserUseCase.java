package pl.gamilife.user.usecase.grantrewardstouser;

import pl.gamilife.api.user.dto.RewardedUserApiDto;
import pl.gamilife.shared.kernel.architecture.UseCase;

public interface GrantRewardsToUserUseCase extends UseCase<GrantRewardsToUserCommand, RewardedUserApiDto> {
}
