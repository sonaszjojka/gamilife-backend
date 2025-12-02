package pl.gamilife.task.repository.jpa;

import pl.gamilife.task.entity.TaskCategory;
import org.springframework.data.jpa.repository.JpaRepository;

public interface TaskCategoryRepositoryJpa extends JpaRepository<TaskCategory, Integer> {

}
