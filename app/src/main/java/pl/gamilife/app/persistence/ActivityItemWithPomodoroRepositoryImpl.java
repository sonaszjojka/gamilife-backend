package pl.gamilife.app.persistence;

import lombok.AllArgsConstructor;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Repository;
import pl.gamilife.app.dto.ActivityItemWithPomodoroFilter;
import pl.gamilife.app.persistence.jpa.JpaActivityItemWithPomodoroRepository;
import pl.gamilife.app.persistence.specification.ActivityItemWithPomodoroSpecificationBuilder;
import pl.gamilife.app.persistence.view.ActivityItemWithPomodoro;
import pl.gamilife.shared.kernel.architecture.Page;

@Repository
@AllArgsConstructor
public class ActivityItemWithPomodoroRepositoryImpl implements ActivityItemWithPomodoroRepository {

    private final JpaActivityItemWithPomodoroRepository jpaActivityItemWithPomodoroRepository;
    private final ActivityItemWithPomodoroSpecificationBuilder activityItemWithPomodoroSpecificationBuilder;

    @Override
    public Page<ActivityItemWithPomodoro> getActivityItemsWithPomodoro(ActivityItemWithPomodoroFilter filter, int page, int size) {
        org.springframework.data.domain.Page<ActivityItemWithPomodoro> result = jpaActivityItemWithPomodoroRepository.findAll(
                activityItemWithPomodoroSpecificationBuilder.build(filter),
                PageRequest.of(page, size, Sort.by("deadlineDate").ascending())
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
