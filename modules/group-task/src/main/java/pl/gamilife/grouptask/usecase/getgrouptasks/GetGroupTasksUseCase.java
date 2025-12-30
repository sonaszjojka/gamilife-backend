package pl.gamilife.grouptask.usecase.getgrouptasks;

import pl.gamilife.shared.kernel.architecture.Page;

import java.util.UUID;

public interface GetGroupTasksUseCase {

    Page<GetGroupTaskDto> execute(UUID groupId, GetGroupTasksRequestFilter request);
}
