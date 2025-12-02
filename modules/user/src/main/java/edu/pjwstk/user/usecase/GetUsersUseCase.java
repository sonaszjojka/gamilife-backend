package edu.pjwstk.user.usecase;

import pl.gamilife.infrastructure.core.architecture.UseCase;
import edu.pjwstk.user.dto.response.GetUsersResult;
import edu.pjwstk.user.dto.service.GetUsersCommand;

public interface GetUsersUseCase extends UseCase<GetUsersCommand, GetUsersResult> {
}
