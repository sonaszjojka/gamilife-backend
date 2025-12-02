package edu.pjwstk.user.usecase.grantrewardstouser;

import edu.pjwstk.api.user.dto.RewardedUserApiDto;
import pl.gamilife.infrastructure.core.architecture.UseCase;

public interface GrantRewardsToUserUseCase extends UseCase<GrantRewardsToUserCommand, RewardedUserApiDto> {
}
