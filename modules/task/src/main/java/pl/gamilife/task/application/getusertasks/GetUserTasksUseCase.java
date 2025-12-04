package pl.gamilife.task.application.getusertasks;

import org.springframework.data.domain.Page;

public interface GetUserTasksUseCase {

    Page<GetUserTasksDto> execute(GetUserTasksFilterDto dto);
}
