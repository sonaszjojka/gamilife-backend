package pl.gamilife.user.usecase;

import pl.gamilife.shared.kernel.architecture.UseCase;
import pl.gamilife.user.dto.response.GetUsersResult;
import pl.gamilife.user.dto.service.GetUsersCommand;

public interface GetUsersUseCase extends UseCase<GetUsersCommand, GetUsersResult> {
}
