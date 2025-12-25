package pl.gamilife.task.infrastructure.persistence.jpa;

import org.jspecify.annotations.NonNull;
import org.jspecify.annotations.Nullable;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.data.jpa.repository.EntityGraph;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import pl.gamilife.task.domain.model.Task;

import java.util.UUID;

public interface JpaTaskRepository extends JpaRepository<Task, UUID>, JpaSpecificationExecutor<Task> {
    @Override
    @EntityGraph(attributePaths = {"category", "difficulty"})
    Page<Task> findAll(@Nullable Specification<Task> spec, @NonNull Pageable pageable);
}
