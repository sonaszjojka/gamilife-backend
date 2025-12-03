package pl.gamilife.grouptask.usecase.getgrouptasks;

import org.springframework.data.domain.Page;

import java.util.UUID;


public interface GetGroupTasksUseCase {

    Page<GetGroupTaskDto> execute(UUID groupId, GetGroupTasksRequestFilter request);
}
