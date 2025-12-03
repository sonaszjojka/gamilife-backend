package pl.gamilife.task.repository.jpa;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import pl.gamilife.task.entity.Task;

import java.util.UUID;

public interface TaskRepositoryJpa extends JpaRepository<Task, UUID>, JpaSpecificationExecutor<Task> {

}
