package pl.gamilife.task.application.getusertasks;

import org.springframework.data.domain.Page;
import pl.gamilife.task.controllers.response.GetUserTasksDto;

public interface GetUserTasksUseCase {

    Page<GetUserTasksDto> execute(GetUserTasksFilterDto dto);
}
