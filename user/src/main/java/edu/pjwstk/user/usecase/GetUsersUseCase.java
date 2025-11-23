package edu.pjwstk.user.usecase;

import edu.pjwstk.core.UseCase;
import edu.pjwstk.user.dto.response.GetUsersResult;
import edu.pjwstk.user.dto.service.GetUsersCommand;

public interface GetUsersUseCase extends UseCase<GetUsersCommand, GetUsersResult> {
}
