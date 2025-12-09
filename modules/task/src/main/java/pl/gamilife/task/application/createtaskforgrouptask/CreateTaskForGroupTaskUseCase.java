package pl.gamilife.task.application.createtaskforgrouptask;

import pl.gamilife.api.task.dto.TaskForGroupTaskRequestDto;
import pl.gamilife.api.task.dto.TaskForGroupTaskResponseDto;
import pl.gamilife.shared.kernel.architecture.UseCase;

public interface CreateTaskForGroupTaskUseCase extends UseCase<CreateTaskForGroupTaskCommand, CreateTaskForGroupTaskResult> {
}
