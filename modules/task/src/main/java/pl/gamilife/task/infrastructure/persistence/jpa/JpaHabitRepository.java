package pl.gamilife.task.infrastructure.persistence.jpa;


import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.domain.Specification;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import pl.gamilife.task.domain.model.Habit;

import java.util.UUID;

public interface JpaHabitRepository extends JpaRepository<Habit, UUID>, JpaSpecificationExecutor<Habit> {
    @Override
    Page<Habit> findAll(Specification<Habit> build, Pageable pageable);
}
