package pl.gamilife.task.infrastructure.persistence;

import lombok.AllArgsConstructor;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Repository;
import pl.gamilife.shared.kernel.architecture.Page;
import pl.gamilife.task.domain.model.Task;
import pl.gamilife.task.domain.model.filter.TaskFilter;
import pl.gamilife.task.domain.port.repository.TaskRepository;
import pl.gamilife.task.infrastructure.persistence.jpa.JpaTaskRepository;
import pl.gamilife.task.infrastructure.persistence.specification.TaskSpecificationBuilder;

import java.util.Optional;
import java.util.UUID;

@Repository
@AllArgsConstructor
public class TaskRepositoryAdapter implements TaskRepository {

    private final JpaTaskRepository jpaTaskRepository;
    private final TaskSpecificationBuilder taskSpecificationBuilder;

    @Override
    public Task save(Task task) {
        return jpaTaskRepository.save(task);
    }

    @Override
    public Optional<Task> findById(UUID taskId) {
        return jpaTaskRepository.findById(taskId);
    }

    @Override
    public void deleteById(UUID taskId) {
        jpaTaskRepository.deleteById(taskId);
    }

    @Override
    public Boolean existsById(UUID taskId) {
        return jpaTaskRepository.existsById(taskId);
    }

    @Override
    public Page<Task> findAll(TaskFilter filter, Integer pageNumber, Integer pageSize) {
        org.springframework.data.domain.Page<Task> result = jpaTaskRepository.findAll(
                taskSpecificationBuilder.build(filter),
                PageRequest.of(
                        pageNumber,
                        pageSize,
                        Sort.by(Sort.Direction.ASC, "deadlineDate")
                )
        );

        return new Page<>(
                result.getContent(),
                result.getTotalElements(),
                result.getTotalPages(),
                result.getNumber(),
                result.getSize()
        );
    }


}
