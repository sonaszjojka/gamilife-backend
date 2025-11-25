package edu.pjwstk.tasks.application.getusertasks;

import org.springframework.data.domain.Page;

import java.util.UUID;

public interface GetUserTasksUseCase {

    Page<GetUserTasksDto> execute(GetUserTasksFilterDto dto);
}
