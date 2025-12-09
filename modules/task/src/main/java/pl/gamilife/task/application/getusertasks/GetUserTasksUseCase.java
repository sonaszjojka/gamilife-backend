package pl.gamilife.task.application.getusertasks;

import pl.gamilife.shared.kernel.architecture.Page;
import pl.gamilife.task.infrastructure.web.response.GetUserTasksDto;

public interface GetUserTasksUseCase {
    Page<GetUserTasksDto> execute(GetUserTasksFilterDto dto);
}
