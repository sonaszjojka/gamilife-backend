package pl.gamilife.task.application.getusertasks;

import pl.gamilife.shared.kernel.architecture.Page;
import pl.gamilife.shared.kernel.architecture.UseCase;

public interface GetUserTasksUseCase extends UseCase<GetUserTasksCommand, Page<GetUserTasksResult>> {
}
