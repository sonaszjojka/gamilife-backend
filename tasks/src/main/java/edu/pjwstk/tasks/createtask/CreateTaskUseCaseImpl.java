package edu.pjwstk.tasks.createtask;

import edu.pjwstk.tasks.repository.TaskRepository;
import edu.pjwstk.tasks.repository.impl.TaskRepositoryImpl;
import org.springframework.stereotype.Component;

@Component
public class CreateTaskUseCaseImpl implements CreateTaskUseCase {

    private final TaskRepository taskRepository;
    private final CreateTaskMapper createTaskMapper;

    public CreateTaskUseCaseImpl(TaskRepositoryImpl taskRepository, CreateTaskMapper createTaskMapper) {
        this.taskRepository = taskRepository;
        this.createTaskMapper = createTaskMapper;
    }

    public CreateTaskResponse execute(CreateTaskRequest request) {
//        taskRepository.save(createTaskMapper.toEntity(request, ))
        //todo: finish when other entities are ready

        return null;
    }
}
