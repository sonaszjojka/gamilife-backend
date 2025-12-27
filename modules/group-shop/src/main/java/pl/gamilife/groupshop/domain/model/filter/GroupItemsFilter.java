package pl.gamilife.groupshop.domain.model.filter;

import jakarta.validation.constraints.NotNull;

import java.util.UUID;

public record GroupItemsFilter(

@NotNull
UUID groupShopId,

@NotNull
Boolean isActive
) {



}
