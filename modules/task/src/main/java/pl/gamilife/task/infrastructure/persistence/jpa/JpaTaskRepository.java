package pl.gamilife.task.infrastructure.persistence.jpa;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import pl.gamilife.task.domain.model.Task;

import java.util.UUID;

public interface JpaTaskRepository extends JpaRepository<Task, UUID>, JpaSpecificationExecutor<Task> {

}
