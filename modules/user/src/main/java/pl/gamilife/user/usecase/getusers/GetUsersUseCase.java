package pl.gamilife.user.usecase.getusers;

import pl.gamilife.shared.kernel.architecture.Page;
import pl.gamilife.shared.kernel.architecture.UseCase;

public interface GetUsersUseCase extends UseCase<GetUsersCommand, Page<GetUsersResult>> {
}
