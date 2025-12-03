package pl.gamilife.task.repository.jpa;

import org.springframework.data.jpa.repository.JpaRepository;
import pl.gamilife.task.entity.TaskCategory;

public interface TaskCategoryRepositoryJpa extends JpaRepository<TaskCategory, Integer> {

}
