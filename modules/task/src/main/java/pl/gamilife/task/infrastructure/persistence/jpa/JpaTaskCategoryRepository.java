package pl.gamilife.task.infrastructure.persistence.jpa;

import org.springframework.data.jpa.repository.JpaRepository;
import pl.gamilife.task.domain.model.TaskCategory;

public interface JpaTaskCategoryRepository extends JpaRepository<TaskCategory, Integer> {

}
