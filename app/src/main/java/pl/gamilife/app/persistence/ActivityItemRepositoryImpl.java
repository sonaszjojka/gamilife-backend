package pl.gamilife.app.persistence;

import lombok.AllArgsConstructor;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Repository;
import pl.gamilife.app.dto.activity.ActivityItemFilter;
import pl.gamilife.app.persistence.jpa.JpaActivityItemRepository;
import pl.gamilife.app.persistence.specification.ActivityItemSpecificationBuilder;
import pl.gamilife.app.persistence.view.ActivityItem;
import pl.gamilife.shared.kernel.architecture.Page;

@Repository
@AllArgsConstructor
public class ActivityItemRepositoryImpl implements ActivityItemRepository {

    private final JpaActivityItemRepository jpaActivityItemRepository;
    private final ActivityItemSpecificationBuilder activityItemSpecificationBuilder;

    @Override
    public Page<ActivityItem> getActivityItemsWithPomodoro(ActivityItemFilter filter, int page, int size) {
        org.springframework.data.domain.Page<ActivityItem> result = jpaActivityItemRepository.findAll(
                activityItemSpecificationBuilder.build(filter),
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
