package pl.gamilife.task.infrastructure.persistence;

import lombok.AllArgsConstructor;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Repository;
import pl.gamilife.shared.kernel.architecture.Page;
import pl.gamilife.task.domain.model.filter.ActivityItemFilter;
import pl.gamilife.task.domain.model.projection.ActivityItem;
import pl.gamilife.task.domain.port.repository.ActivityItemRepository;
import pl.gamilife.task.infrastructure.persistence.jpa.JpaActivityItemRepository;
import pl.gamilife.task.infrastructure.persistence.specification.ActivityItemSpecificationBuilder;

@Repository
@AllArgsConstructor
public class ActivityItemRepositoryAdapter implements ActivityItemRepository {

    private final JpaActivityItemRepository jpaActivityItemRepository;
    private final ActivityItemSpecificationBuilder activityItemSpecificationBuilder;

    @Override
    public Page<ActivityItem> findAll(ActivityItemFilter filter, int page, int size) {
        org.springframework.data.domain.Page<ActivityItem> all = jpaActivityItemRepository.findAll(
                activityItemSpecificationBuilder.build(filter),
                PageRequest.of(page, size, Sort.by(Sort.Direction.ASC, "deadlineDate", "deadlineTime")));

        return new Page<>(
                all.getContent(),
                all.getTotalElements(),
                all.getTotalPages(),
                all.getNumber(),
                all.getSize()
        );
    }
}
